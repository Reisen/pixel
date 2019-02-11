import React     from 'react';
import TextInput from '../../../../components/TextInput'
import styles    from './SearchSideBar.module.css';

/* -------------------------------------------------------------------------- */

interface TagProps {
    tag: string;
    category?: string;
}

const Tag = (props: TagProps) => (
    <a href="#" className={styles[`Category--${props.category}`]}>
        {props.tag}
    </a>
);

/* -------------------------------------------------------------------------- */

interface Props {
    tags?: string[];
}

const SearchSideBar = (props: Props) => (
    <div className={styles.SearchSideBar}>
        <TextInput placeholder="Search" />

        <div className={styles.TagList}>
            {
                !props.tags ? null : props.tags.map(tag => (
                    <Tag tag={tag}/>
                ))
            }
            <Tag tag="yoshimitsu" category="artist" />
            <Tag tag="fighter" />
        </div>
    </div>
);

export default SearchSideBar;
