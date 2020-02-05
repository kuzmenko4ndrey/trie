/*!
 * \file Trie.h
 * \brief Trie data structure
 */
#ifndef TRIE_H
#define TRIE_H

// STL headers
#include <algorithm>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <vector>

namespace internals
{
	// Associative container for key->node association
	template<typename KeyT, typename ValueT>
	class VectorAssociativeContainer
	{
	public:
		bool empty() const
		{
			return m_Vector.empty();
		}

		template<typename T, typename S>
		auto emplace( T&& key, S&& value )
		{
			auto it = find( key );
			if ( it != std::end( m_Vector ) )
			{
				return std::make_pair( it, false );
			}
			m_Vector.emplace_back( KeyT( std::forward<T>( key ) ), ValueT( std::forward<S>( value ) ) );
			return std::make_pair( std::next( std::begin( m_Vector ), m_Vector.size() - 1 ), true );
		}

		auto begin()
		{
			return std::begin( m_Vector );
		}

		auto end()
		{
			return std::end( m_Vector );
		}

		auto begin() const
		{
			return std::begin( m_Vector );
		}

		auto end() const
		{
			return std::end( m_Vector );
		}

		template<typename T>
		ValueT& operator[]( T&& key )
		{
			auto it = find( key );
			if ( it != std::end( m_Vector ) )
			{
				return it->second;
			}

			m_Vector.emplace_back( std::forward<T>( key ), ValueT{} );
			return m_Vector.back().second;
		}

		auto find( const KeyT& key )
		{
			auto it = std::begin( m_Vector );
			for ( ; it != std::end( m_Vector ) && it->first != key; ++it );
			return it;
		}

		auto find( const KeyT& key ) const
		{
			auto it = std::begin( m_Vector );
			for ( ; it != std::end( m_Vector ) && it->first != key; ++it );
			return it;
		}

		void swap( VectorAssociativeContainer& another )
		{
			m_Vector.swap( another.m_Vector );
		}

	private:
		std::vector<std::pair<const KeyT, ValueT>> m_Vector;
	};
} // namespace internals

template<typename KeyBaseT, typename ValueT, typename Hash = std::hash<KeyBaseT>>
class Trie
{
public:
	using size_type = std::size_t;

private:
	using KeysSequence = std::vector<KeyBaseT>;

	static constexpr std::size_t NPOS = std::numeric_limits<std::size_t>::max();

	struct Node
	{
		std::size_t parentIndex = NPOS;
		KeysSequence subseq{};
		std::size_t leafIndex = NPOS;
		std::conditional_t<sizeof( KeyBaseT ) == 1,
						   internals::VectorAssociativeContainer<KeyBaseT, std::size_t>,
						   std::unordered_map<KeyBaseT, std::size_t, Hash>> children{};
	};

	struct Leaf
	{
		std::size_t parentIndex = NPOS;
		ValueT value{};
	};

public:
	// Forward Iterator
	template<bool IsConst>
	class IteratorTemplate : public std::iterator<std::forward_iterator_tag,
										typename std::conditional_t<IsConst, const ValueT, ValueT>>
	{
		friend class Trie;

		using TriePtr = typename std::conditional_t<IsConst, const Trie*, Trie*>;

		struct FromLeafType {};
		static constexpr FromLeafType FROM_LEAF{};
		struct FromNodeType {};
		static constexpr FromNodeType FROM_NODE{};

		explicit IteratorTemplate( TriePtr pTrie, std::size_t nodeIndex, FromNodeType )
			: m_pTrie{ pTrie },
			  m_LeafIndex{ m_pTrie->nextLeaf( nodeIndex ) }
		{
		}

		explicit IteratorTemplate( TriePtr pTrie, std::size_t leafIndex, FromLeafType )
			: m_pTrie{ pTrie },
			  m_LeafIndex{ leafIndex }
		{
		}

		explicit IteratorTemplate( TriePtr pTrie )
			: m_pTrie{ pTrie },
			  m_LeafIndex{ Trie::NPOS }
		{
		}

	public:
		IteratorTemplate()
			: m_pTrie{ nullptr },
			  m_LeafIndex{ Trie::NPOS }
		{
		}

		template<bool C = IsConst>
		std::enable_if_t<!C && C == IsConst, ValueT>& operator*()
		{
			return m_pTrie->m_Leafs[ m_LeafIndex ].value;
		}

		template<typename Container>
		Container key() const
		{
			return m_pTrie->key<Container>( m_LeafIndex );
		}

		const ValueT& operator*() const
		{
			return m_pTrie->m_Leafs[ m_LeafIndex ].value;
		}

		template<bool C = IsConst>
		std::enable_if_t<!C && C == IsConst, ValueT>* operator->()
		{
			return &m_pTrie->m_Leafs[ m_LeafIndex ].value;
		}

		const ValueT* operator->() const
		{
			return &m_pTrie->m_Leafs[ m_LeafIndex ].value;
		}

		IteratorTemplate& operator++()
		{
			m_LeafIndex = m_pTrie->nextLeaf( m_pTrie->m_Leafs[ m_LeafIndex ].parentIndex );
			return *this;
		}

		IteratorTemplate operator++( int )
		{
			iterator oldIter{ m_pTrie, m_LeafIndex, FROM_LEAF };
			m_LeafIndex = m_pTrie->nextLeaf( m_pTrie->m_Leafs[ m_LeafIndex ].parentIndex );
			return oldIter;
		}

		template<bool IsAnotherConst>
		bool operator==( IteratorTemplate<IsAnotherConst> another ) const
		{
			return m_pTrie == another.m_pTrie && m_LeafIndex == another.m_LeafIndex;
		}

		template<bool IsAnotherConst>
		bool operator!=( IteratorTemplate<IsAnotherConst> another ) const
		{
			return !operator==( another );
		}

		template<bool C = IsConst>
		operator std::enable_if_t<!C && C == IsConst, IteratorTemplate<true>>() const
		{
			return IteratorTemplate<true>( m_pTrie, m_LeafIndex, IteratorTemplate<true>::FROM_LEAF );
		}

	private:
		TriePtr m_pTrie;
		std::size_t m_LeafIndex;
	};

	using iterator = IteratorTemplate<false>;
	using const_iterator = IteratorTemplate<true>;

	Trie()
		: m_Nodes( 1 )
	{
	}

	template<typename Container>
	Trie( std::initializer_list<std::pair<Container, ValueT>>&& list )
		: m_Nodes( 1 )
	{
		for ( auto&& p : list )
		{
			insertOrGet( std::begin( p.first ), std::end( p.first ) ).value = std::move( p.second );
		}
	}

	template<typename Container>
	Trie( const std::initializer_list<std::pair<Container, ValueT>>& list )
		: m_Nodes( 1 )
	{
		for ( const auto& p : list )
		{
			insertOrGet( std::begin( p.first ), std::end( p.first ) ).value = p.second;
		}
	}

	template<typename CharT>
	Trie( std::initializer_list<std::pair<const CharT*, ValueT>> list )
		: m_Nodes( 1 )
	{
		for ( auto&& p : list )
		{
			insertOrGet( p.first ).value = std::move( p.second );
		}
	}

	Trie( std::initializer_list<std::pair<const KeyBaseT*, ValueT>> list )
		: m_Nodes( 1 )
	{
		for ( auto&& p : list )
		{
			insertOrGet( p.first ).value = std::move( p.second );
		}
	}

	template<typename Container>
	ValueT& operator[]( const Container& sequence )
	{
		return insertOrGet( std::begin( sequence ), std::end( sequence ) ).value;
	}

	template<typename CharT>
	ValueT& operator[]( const CharT* str )
	{
		return insertOrGet( str ).value;
	}

	iterator begin()
	{
		return iterator( this, 0, iterator::FROM_NODE );
	}

	iterator end()
	{
		return iterator( this );
	}

	const_iterator begin() const
	{
		return const_iterator( this, 0, const_iterator::FROM_NODE );
	}

	const_iterator end() const
	{
		return const_iterator( this );
	}

	const_iterator cbegin() const
	{
		return const_iterator( this, 0, const_iterator::FROM_NODE );
	}

	const_iterator cend() const
	{
		return const_iterator( this );
	}

	template<typename Container>
	ValueT& at( const Container& sequence )
	{
		return at( std::begin( sequence ), std::end( sequence ) );
	}

	template<typename Container>
	const ValueT& at( const Container& sequence ) const
	{
		return at( std::begin( sequence ), std::end( sequence ) );
	}

	template<typename CharT>
	ValueT& at( const CharT* str )
	{
		return insertOrGet( str ).value;
	}

	template<typename CharT>
	const ValueT& at( const CharT* str ) const
	{
		auto it = find( str );
		if ( it == end() )
		{
			throw std::out_of_range( "Sequence is not present" );
		}
		return *it;
	}

	template<typename Iterator>
	ValueT& at( Iterator first, Iterator last )
	{
		return insertOrGet( first, last ).value;
	}

	template<typename Iterator>
	const ValueT& at( Iterator first, Iterator last ) const
	{
		auto it = find( first, last );
		if ( it == end() )
		{
			throw std::out_of_range( "Sequence is not present" );
		}
		return *it;
	}

	template<typename Container>
	const_iterator find( const Container& sequence ) const
	{
		return find( std::begin( sequence ), std::end( sequence ) );
	}

	template<typename CharT>
	const_iterator find( const CharT* str ) const;

	template<typename Iterator>
	const_iterator find( Iterator first, Iterator last ) const;

	template<typename Container>
	size_type count( const Container& sequence ) const
	{
		return count( std::begin( sequence ), std::end( sequence ) );
	}

	template<typename Iterator>
	size_type count( Iterator first, Iterator last ) const
	{
		return find( first, last ) != end();
	}

	template<typename Iterator>
	void insert( Iterator first, Iterator last, const ValueT& value )
	{
		insertOrGet( first, last ).value = value;
	}

	size_type size() const
	{
		return m_Leafs.size();
	}

	bool empty() const
	{
		return m_Leafs.empty();
	}

	void clear()
	{
		std::vector<Node>( 1 ).swap( m_Nodes );
		m_Leafs.clear();
	}

private:
	std::size_t nextLeaf( std::size_t startNodeIndex ) const;
	std::size_t prevLeaf( std::size_t startNodeIndex ) const;

	template<typename Container>
	Container key( std::size_t leafIndex ) const;

	std::size_t splitNode( std::size_t nodeIndex, const typename KeysSequence::iterator& subseqIt );

	template<typename Iterator>
	Leaf& insertOrGet( Iterator first, Iterator last );

	template<typename CharT>
	Leaf& insertOrGet( const CharT* str );

	std::vector<Node> m_Nodes;
	std::vector<Leaf> m_Leafs;
};

template<typename KeyBaseT, typename ValueT, typename Hash>
constexpr std::size_t Trie<KeyBaseT, ValueT, Hash>::NPOS;

template<typename KeyBaseT, typename ValueT, typename Hash>
template<typename CharT>
typename Trie<KeyBaseT, ValueT, Hash>::const_iterator Trie<KeyBaseT, ValueT, Hash>::find( const CharT* str ) const
{
	std::size_t currentNodeIndex = 0;
	auto subseqIt = std::begin( m_Nodes[ currentNodeIndex ].subseq );
	for ( ; *str; ++str, ++subseqIt )
	{
		if ( subseqIt == std::end( m_Nodes[ currentNodeIndex ].subseq ) )
		{
			auto nextIt = m_Nodes[ currentNodeIndex ].children.find( *str );
			if ( nextIt == std::end( m_Nodes[ currentNodeIndex ].children ) )
			{
				return cend();
			}
			currentNodeIndex = nextIt->second;
			subseqIt = std::begin( m_Nodes[ currentNodeIndex ].subseq );
		}
		else if ( *subseqIt != *str )
		{
			return cend();
		}
	}

	if ( m_Nodes[ currentNodeIndex ].leafIndex == NPOS || subseqIt != std::end( m_Nodes[ currentNodeIndex ].subseq ) )
	{
		return cend();
	}

	return const_iterator( this, m_Nodes[ currentNodeIndex ].leafIndex, const_iterator::FROM_LEAF );
}

template<typename KeyBaseT, typename ValueT, typename Hash>
template<typename Iterator>
typename Trie<KeyBaseT, ValueT, Hash>::const_iterator Trie<KeyBaseT, ValueT, Hash>::find( Iterator first, Iterator last ) const
{
	std::size_t currentNodeIndex = 0;
	auto subseqIt = std::begin( m_Nodes[ currentNodeIndex ].subseq );
	for ( auto it = first; it != last; ++it, ++subseqIt )
	{
		if ( subseqIt == std::end( m_Nodes[ currentNodeIndex ].subseq ) )
		{
			auto nextIt = m_Nodes[ currentNodeIndex ].children.find( *it );
			if ( nextIt == std::end( m_Nodes[ currentNodeIndex ].children ) )
			{
				return cend();
			}
			currentNodeIndex = nextIt->second;
			subseqIt = std::begin( m_Nodes[ currentNodeIndex ].subseq );
		}
		else if ( *subseqIt != *it )
		{
			return cend();
		}
	}

	if ( m_Nodes[ currentNodeIndex ].leafIndex == NPOS || subseqIt != std::end( m_Nodes[ currentNodeIndex ].subseq ) )
	{
		return cend();
	}

	return const_iterator( this, m_Nodes[ currentNodeIndex ].leafIndex, const_iterator::FROM_LEAF );
}

template<typename KeyBaseT, typename ValueT, typename Hash>
std::size_t Trie<KeyBaseT, ValueT, Hash>::nextLeaf( std::size_t startNodeIndex ) const
{
	// Traverse to leftmost subtree
	if ( !m_Nodes[ startNodeIndex ].children.empty() )
	{
		do
		{
			startNodeIndex = std::begin( m_Nodes[ startNodeIndex ].children )->second;
		}
		while ( m_Nodes[ startNodeIndex ].leafIndex == NPOS );
		return m_Nodes[ startNodeIndex ].leafIndex;
	}

	// Traverse to the right subtree of a non-fully traversed ancestor
	while ( m_Nodes[ startNodeIndex ].parentIndex != NPOS &&
			std::next( m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].children.find( m_Nodes[ startNodeIndex ].subseq[ 0 ] ) ) ==
				std::end( m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].children ) )
	{
		startNodeIndex = m_Nodes[ startNodeIndex ].parentIndex;
	}

	// Traverse to leftmost subtree
	if ( m_Nodes[ startNodeIndex ].parentIndex != NPOS )
	{
		startNodeIndex = std::next( m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].children.find( m_Nodes[ startNodeIndex ].subseq[ 0 ] ) )->second;
		while ( m_Nodes[ startNodeIndex ].leafIndex == NPOS )
		{
			startNodeIndex = std::begin( m_Nodes[ startNodeIndex ].children )->second;
		}
		return m_Nodes[ startNodeIndex ].leafIndex;
	}
	return NPOS;
}

template<typename KeyBaseT, typename ValueT, typename Hash>
std::size_t Trie<KeyBaseT, ValueT, Hash>::prevLeaf( std::size_t startNodeIndex ) const
{
	while ( m_Nodes[ startNodeIndex ].parentIndex != NPOS && m_Leafs[ m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].leafIndex ] == NPOS &&
			m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].children.find( m_Nodes[ startNodeIndex ].subseq[ 0 ] ) ==
			std::begin( m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].children ) )
	{
		startNodeIndex = m_Nodes[ startNodeIndex ].parentIndex;
	}

	if ( m_Nodes[ startNodeIndex ].parentIndex != NPOS )
	{
		if ( m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].leafIndex != NPOS )
		{
			return m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].leafIndex;
		}

		startNodeIndex = std::prev( m_Nodes[ m_Nodes[ startNodeIndex ].parentIndex ].children.find( m_Nodes[ startNodeIndex ].subseq[ 0 ] ) )->second;
		while ( !m_Nodes[ startNodeIndex ].children.empty() )
		{
			startNodeIndex = m_Nodes[ startNodeIndex ].children.rbegin()->second;
		}
		return m_Nodes[ startNodeIndex ].leafIndex;
	}
	return NPOS;
}

template<typename KeyBaseT, typename ValueT, typename Hash>
template<typename Iterator>
typename Trie<KeyBaseT, ValueT, Hash>::Leaf& Trie<KeyBaseT, ValueT, Hash>::insertOrGet( Iterator first, Iterator last )
{
	std::size_t curNodeIndex = 0;
	auto subseqIt = std::begin( m_Nodes[ curNodeIndex ].subseq );
	auto subseqEndIt = std::end( m_Nodes[ curNodeIndex ].subseq );
	for ( auto it = first; it != last; ++it, ++subseqIt )
	{
		while ( subseqIt != subseqEndIt && it != last && *subseqIt == *it )
		{
			++it;
			++subseqIt;
		}

		if ( it == last )
		{
			if ( subseqIt != subseqEndIt )
			{
				curNodeIndex = splitNode( curNodeIndex, subseqIt );
			}
			break;
		}
		else if ( subseqIt == subseqEndIt )
		{
			auto& nextNodeIndex = m_Nodes[ curNodeIndex ].children.emplace( *it, NPOS ).first->second;
			if ( nextNodeIndex == NPOS )
			{
				m_Nodes.emplace_back( Node{ curNodeIndex, KeysSequence( it, last ) } );
				curNodeIndex = nextNodeIndex = m_Nodes.size() - 1;
				break;
			}
			curNodeIndex = nextNodeIndex;
			subseqIt = std::begin( m_Nodes[ nextNodeIndex ].subseq );
			subseqEndIt = std::end( m_Nodes[ nextNodeIndex ].subseq );
		}
		else if ( *subseqIt != *it )
		{
			// Split
			curNodeIndex = splitNode( curNodeIndex, subseqIt );
			auto newNodeIndex = m_Nodes.size();
			m_Nodes.emplace_back( Node{ curNodeIndex, KeysSequence( it, last ) } );
			curNodeIndex = m_Nodes[ curNodeIndex ].children[ *it ] = newNodeIndex;
			break;
		}
	}

	if ( m_Nodes[ curNodeIndex ].leafIndex == NPOS )
	{
		m_Nodes[ curNodeIndex ].leafIndex = m_Leafs.size();
		m_Leafs.emplace_back( Leaf{ curNodeIndex } );
	}

	return m_Leafs[ m_Nodes[ curNodeIndex ].leafIndex ];
}

template<typename KeyBaseT, typename ValueT, typename Hash>
template<typename CharT>
typename Trie<KeyBaseT, ValueT, Hash>::Leaf& Trie<KeyBaseT, ValueT, Hash>::insertOrGet( const CharT* str )
{
	std::size_t curNodeIndex = 0;
	auto subseqIt = std::begin( m_Nodes[ curNodeIndex ].subseq );
	auto subseqEndIt = std::end( m_Nodes[ curNodeIndex ].subseq );
	for ( ; *str; ++str, ++subseqIt )
	{
		while ( subseqIt != subseqEndIt && *str && *subseqIt == *str )
		{
			++str;
			++subseqIt;
		}

		if ( !*str )
		{
			if ( subseqIt != subseqEndIt )
			{
				curNodeIndex = splitNode( curNodeIndex, subseqIt );
			}
			break;
		}
		else if ( subseqIt == subseqEndIt )
		{
			auto& nextNodeIndex = m_Nodes[ curNodeIndex ].children.emplace( *str, NPOS ).first->second;
			if ( nextNodeIndex == NPOS )
			{
				KeysSequence tail;
				while ( *str )
				{
					tail.push_back( *str++ );
				}
				m_Nodes.emplace_back( Node{ curNodeIndex, std::move( tail ) } );
				curNodeIndex = nextNodeIndex = m_Nodes.size() - 1;
				break;
			}
			curNodeIndex = nextNodeIndex;
			subseqIt = std::begin( m_Nodes[ curNodeIndex ].subseq );
			subseqEndIt = std::end( m_Nodes[ curNodeIndex ].subseq );
		}
		else if ( *subseqIt != *str )
		{
			// Split
			auto curChar = *str;
			KeysSequence tail;
			while ( *str )
			{
				tail.push_back( *str++ );
			}

			curNodeIndex = splitNode( curNodeIndex, subseqIt );
			auto newNodeIndex = m_Nodes.size();
			m_Nodes.emplace_back( Node{ curNodeIndex, std::move( tail ) } );
			curNodeIndex = m_Nodes[ curNodeIndex ].children[ curChar ] = newNodeIndex;
			break;
		}
	}

	if ( m_Nodes[ curNodeIndex ].leafIndex == NPOS )
	{
		m_Nodes[ curNodeIndex ].leafIndex = m_Leafs.size();
		m_Leafs.emplace_back( Leaf{ curNodeIndex } );
	}

	return m_Leafs[ m_Nodes[ curNodeIndex ].leafIndex ];
}

template<typename KeyBaseT, typename ValueT, typename Hash>
template<typename Container>
Container Trie<KeyBaseT, ValueT, Hash>::key( std::size_t leafIndex ) const
{
	Container result;
	for ( auto currentIndex = m_Leafs[ leafIndex ].parentIndex;
		  currentIndex != NPOS && m_Nodes[ currentIndex ].parentIndex != NPOS;
		  currentIndex = m_Nodes[ currentIndex ].parentIndex )
	{
		auto& subseq = m_Nodes[ currentIndex ].subseq;
		result.insert( std::end( result ), std::rbegin( subseq ), std::rend( subseq ) );
	}
	std::reverse( std::begin( result ), std::end( result ) );
	return result;
}

template<typename KeyBaseT, typename ValueT, typename Hash>
std::size_t Trie<KeyBaseT, ValueT, Hash>::splitNode( std::size_t nodeIndex, const typename KeysSequence::iterator& subseqIt )
{
	auto newNodeWithOldTailIndex = m_Nodes.size();
	m_Nodes.emplace_back( Node{ nodeIndex, KeysSequence( subseqIt, std::end( m_Nodes[ nodeIndex ].subseq ) ) } );

	m_Nodes.back().children.swap( m_Nodes[ nodeIndex ].children );

	if ( m_Nodes[ nodeIndex ].leafIndex != NPOS )
	{
		m_Nodes.back().leafIndex = m_Nodes[ nodeIndex ].leafIndex;
		m_Nodes[ nodeIndex ].leafIndex = NPOS;
		m_Leafs[ m_Nodes.back().leafIndex ].parentIndex = newNodeWithOldTailIndex;
	}

	for ( auto& child : m_Nodes.back().children )
	{
		m_Nodes[ child.second ].parentIndex = newNodeWithOldTailIndex;
	}

	m_Nodes[ nodeIndex ].subseq.erase( subseqIt, std::end( m_Nodes[ nodeIndex ].subseq ) );
	m_Nodes[ nodeIndex ].children.emplace( m_Nodes.back().subseq.front(), newNodeWithOldTailIndex );

	return nodeIndex;
}

#endif // TRIE_H